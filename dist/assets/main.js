// The page has no server: Arizona's static client runs its event attributes
// (the theme switch, the tabs, the dialog, copy, Picture-in-Picture) in the
// browser.
import { hooks, setAll, start } from './arizona-static.min.js';

const THEME_KEY = 'arizona-theme';

// Pop-out needs Document Picture-in-Picture, which only some browsers have.
hooks.NeedsPip = {
    mounted() {
        if (!('documentPictureInPicture' in window)) this.el.hidden = true;
    },
};

start();

// The switch only sets the page's `theme` slot; remembering the choice across
// visits is this script's job, from the event the switch dispatches.
try {
    const saved = localStorage.getItem(THEME_KEY);
    if (saved) setAll('theme', saved);
} catch {
    // Storage blocked: the page keeps following the OS.
}
document.addEventListener('arizona:theme', (e) => {
    try {
        localStorage.setItem(THEME_KEY, /** @type {CustomEvent} */ (e).detail.theme);
    } catch {
        // Storage blocked: the choice lasts for this visit.
    }
});
