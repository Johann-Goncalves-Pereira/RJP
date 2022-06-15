/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./index.html", "./src/**/*.{vue,js,ts,jsx,tsx,elm}"],
  theme: {
    colors: {
      //& Surface
      "primary-10": "hsla(var(--clr-primary-10-hsl) / <alpha-value>)",
      "primary-20": "hsla(var(--clr-primary-20-hsl) / <alpha-value>)",
      "primary-30": "hsla(var(--clr-primary-30-hsl) / <alpha-value>)",
      "primary-40": "hsla(var(--clr-primary-40-hsl) / <alpha-value>)",
      "primary-50": "hsla(var(--clr-primary-50-hsl) / <alpha-value>)",
      "primary-60": "hsla(var(--clr-primary-60-hsl) / <alpha-value>)",
      // & Text
      "secondary-10": "hsla(var(--clr-secondary-10-hsl) / <alpha-value>)",
      "secondary-20": "hsla(var(--clr-secondary-20-hsl) / <alpha-value>)",
      "secondary-30": "hsla(var(--clr-secondary-30-hsl) / <alpha-value>)",
      "secondary-40": "hsla(var(--clr-secondary-40-hsl) / <alpha-value>)",
    },
    borderRadius: {
      sm: "var(--rounded-sm)",
    },
  },
  plugins: [],
  corePlugins: {
    preflight: false,
  },
};
