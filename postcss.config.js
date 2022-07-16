module.exports = {
  plugins: {
    doiuse: {},
    cssnano: {
      preset: [
        "advanced",
        process.env.NODE_ENV === "production"
          ? { cssDeclarationSorter: false }
          : {
              cssDeclarationSorter: false,
              convertValues: false,
              reduceIdents: false,
            },
      ],
    },
    "postcss-custom-media": {},
    "postcss-custom-selectors": {},
    "postcss-inline-svg": {},
    "postcss-jit-props": require("open-props"),
    "postcss-plugin": {},
    "postcss-pseudo-class-enter": {},
    "postcss-utilities": {},
    "postcss-will-change": {},
    tailwindcss: {},
    "rucksack-css": {},
  },
};
