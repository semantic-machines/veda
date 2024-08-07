{
  "ignore": [
    "sw-lite.js",
    "sw-simple.js",
    "tests/**",
    "**/node_modules/**",
    "watch/**",
    "rollup.config.js",
    "Gulpfile.js"
  ],
  "plugins": [
    "@babel/plugin-transform-instanceof",
    "@babel/plugin-transform-modules-systemjs"
  ],
  "presets": [
    [
      "@babel/preset-env",
      {
        "useBuiltIns": "entry",
        "corejs": 3,
        "targets": "> 0.25%, not dead"
      }
    ],
    [
      "minify",
      {
        "booleans": true,
        "builtIns": false,
        "consecutiveAdds": true,
        "deadcode": true,
        "evaluate": false,
        "flipComparisons": true,
        "guards": true,
        "infinity": true,
        "mangle": true,
        "memberExpressions": true,
        "mergeVars": true,
        "numericLiterals": true,
        "propertyLiterals": true,
        "regexpConstructors": true,
        "removeConsole": false,
        "removeDebugger": true,
        "removeUndefined": true,
        "replace": true,
        "simplify": true,
        "simplifyComparisons": true,
        "typeConstructors": true,
        "undefinedToVoid": true
      }
    ]
  ],
  "comments": false
}
