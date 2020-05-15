import commonjs from "@rollup/plugin-commonjs";
import resolve from "@rollup/plugin-node-resolve";

export default {
  input: "output/Main/index.js",
  output: {
    file: "dist/index.js",
    format: "cjs",
  },

  // these are all in the node standard library
  external: ["http", "https", "querystring", "url", "util"],
  plugins: [commonjs(), resolve()],
};
