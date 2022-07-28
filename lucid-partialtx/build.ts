import { build, emptyDir } from "https://deno.land/x/dnt@0.26.0/mod.ts";
import * as esbuild from "https://deno.land/x/esbuild@v0.14.49/mod.js";
import packageInfo from "./package.json" assert { type: "json" };

await emptyDir("./dist");
Deno.copyFileSync("../LICENSE", "dist/LICENSE");
Deno.copyFileSync("../README.md", "dist/README.md");

// Prepare the NPM package, which will contain both a node version and a web version.

// This creates the node version.
await build({
  entryPoints: ["./mod.ts"],
  outDir: "./dist",
  test: false,
  scriptModule: false,
  typeCheck: false,
  shims: {},
  mappings: {
    "https://deno.land/x/lucid@0.5.2/mod.ts": {
      name: "lucid-cardano",
      version: "^0.5.2",
      peerDependency: true,
    },
  },
  package: {
    ...packageInfo,
    engines: {
      node: ">=14",
    },
    main: "./esm/mod.js",
    type: "module",
  },
});

// This creates the web modules from the outputted node package.
await esbuild.build({
  bundle: true,
  minify: true,
  format: "esm",
  entryPoints: ["./dist/esm/mod.js"],
  outfile: "./dist/web/mod.js",
  plugins: [esbuildMappings({
    "lucid-cardano": "https://unpkg.com/lucid-cardano@0.5.2/web/mod.js",
  })],
});
esbuild.stop();

/**
This is a handrolled import map plugin, similar to webpack and dnt. I dislike dependencies.

The idea is that the npm imports (such as `lucid-cardano`) as produced inside `dist/esm/mod.js`
have to be replaced with their web versions in the `dist/web` build result.

You could do a runtime check for the environment and dynamically import the proper dependency, making sure
webpack/esbuild/whatever doesn't try to accidentally inline them....

No, that's really hacky - I'd much rather do it at the bundler level.

Note that the ability for this library to work with node, deno, and web are still properly preserved.
*/

function esbuildMappings(mappings: Record<string, string>): esbuild.Plugin {
  const name = "esbuild-mappings";
  const filter = new RegExp(
    String.raw`^(?:${Object.keys(mappings).join("|")})`,
  );

  return {
    name,
    setup: (build) => {
      build.onResolve({ filter }, (args) => ({
        path: args.path,
        namespace: name,
        pluginData: {
          resolveDir: args.resolveDir,
        },
      }));

      build.onLoad({ filter, namespace: name }, (args) => {
        const newImportPath = mappings[args.path];
        const contents = `
          export * from "${newImportPath}";
        `;
        return { contents, resolveDir: args.pluginData.resolveDir };
      });
    },
  };
}
