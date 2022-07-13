import { defineConfig } from "vite";
import elm from "vite-plugin-elm";
import legacy from '@vitejs/plugin-legacy'

export default defineConfig({
  plugins: [elm(),
    legacy({
      targets: ['defaults', 'not IE 11']
    })],
});
