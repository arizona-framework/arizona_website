import Arizona from '/assets/js/arizona.min.js';

globalThis.arizona = new Arizona();
arizona.connect();

document.addEventListener('arizonaEvent', (event) => {
    const { type, data } = event.detail;
    switch (type) {
        case 'status': {
            if (data.status === 'connected') {
                setTimeout(() => {
                    window.Prism.highlightAll();
                }, 5)
            }
            break;
        }
        case 'reply': {
            if (typeof data?.reload !== 'string') return
            switch (data.reload) {
                case 'erl':
                    window.location.reload();
                    break;
                case 'css':
                    document.querySelectorAll('link[rel="stylesheet"]').forEach((link) => {
                        const href = link.href.split('?')[0];
                        link.href = href + '?t=' + Date.now();
                    });
                    break;
            }
            break;
        }
    }
});
