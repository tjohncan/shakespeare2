// Public-facing preview shell. The form behind the modal is inert; the
// dialog opens on load and is the visitor's only interaction surface —
// either start the OAuth flow or exit back to the configured parent site.
// No WebSocket, no Ollama traffic.

const dialog = document.getElementById('login-modal');

if (dialog && typeof dialog.showModal === 'function') {
    dialog.showModal();
    // ESC fires 'cancel' on a <dialog>; preventDefault keeps the modal
    // up. Dismissing it would strand the visitor on a disabled form with
    // no way back to the OAuth flow, which is worse than the lost ESC.
    dialog.addEventListener('cancel', function (e) {
        e.preventDefault();
    });
} else {
    // <dialog> + showModal is universally supported in modern browsers.
    // The redirect is the least-bad fallback when it isn't: the visitor
    // sees the auth-server's login page instead of the preview, but
    // they're not stuck on an inert form with no affordance.
    window.location = '/login';
}
