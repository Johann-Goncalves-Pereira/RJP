/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./index.html", "./src/**/*.{vue,js,ts,jsx,tsx,elm}"],
  theme: {
    colors: {
      //& Surface
      "surface-100": "hsla(var(--clr-surface-100-hsl) / <alpha-value>)",
      "surface-200": "hsla(var(--clr-surface-200-hsl) / <alpha-value>)",
      "surface-300": "hsla(var(--clr-surface-300-hsl) / <alpha-value>)",
      "surface-400": "hsla(var(--clr-surface-400-hsl) / <alpha-value>)",
      "surface-500": "hsla(var(--clr-surface-500-hsl) / <alpha-value>)",
      "surface-600": "hsla(var(--clr-surface-600-hsl) / <alpha-value>)",
      "surface-700": "hsla(var(--clr-surface-700-hsl) / <alpha-value>)",
      "surface-800": "hsla(var(--clr-surface-800-hsl) / <alpha-value>)",
      "surface-900": "hsla(var(--clr-surface-900-hsl) / <alpha-value>)",
      // & Accent
      "accent-100": "hsla(var(--clr-accent-100-hsl) / <alpha-value>)",
      "accent-200": "hsla(var(--clr-accent-200-hsl) / <alpha-value>)",
      "accent-300": "hsla(var(--clr-accent-300-hsl) / <alpha-value>)",
      "accent-400": "hsla(var(--clr-accent-400-hsl) / <alpha-value>)",
      "accent-500": "hsla(var(--clr-accent-500-hsl) / <alpha-value>)",
      "accent-600": "hsla(var(--clr-accent-600-hsl) / <alpha-value>)",
      "accent-700": "hsla(var(--clr-accent-700-hsl) / <alpha-value>)",
      "accent-800": "hsla(var(--clr-accent-800-hsl) / <alpha-value>)",
      "accent-900": "hsla(var(--clr-accent-900-hsl) / <alpha-value>)",
    },
    borderRadius: {
      sm: "var(--rounded-sm)",
      full: "var(--rounded-full)",
    },
    fontFamily: {
      mono: "var(--font-mono)",
      serif: "var(--font-serif)",
    },
    fontWeight: {
      100: 100,
      200: 200,
      300: 300,
      400: 400,
      500: 500,
      600: 600,
      700: 700,
      800: 800,
      900: 900,
    },
    screens: {
      xxs: "240px",
      xs: "320px",
      sm: "480px",
      md: "768px",
      lg: "1024px",
      xl: "1440px",
      xxl: "1920px",
    },
    extend: {
      gridTemplateColumns: {
        "fit-20": "repeat(auto-fit, minmax(20rem, 1fr))",
      },
      width: {
        gp: "clamp(45ch,50vw + 1rem,75ch)",
        "min-base": "min(100%, 100vw_-_2rem)",
      },
    },
  },
  plugins: [],
  corePlugins: {
    preflight: false,
  },
};
