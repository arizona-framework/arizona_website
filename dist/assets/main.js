// Scroll animations with Intersection Observer
window.initScrollAnimations = () => {
    const observer = new IntersectionObserver((entries) => {
        entries.forEach((entry) => {
            if (entry.isIntersecting) {
                entry.target.classList.remove('opacity-0', 'translate-y-8');
                entry.target.classList.add('opacity-100', 'translate-y-0');
            }
        });
    }, {
        threshold: 0.1,
        rootMargin: '-50px 0px'
    });

    document.querySelectorAll('.animate-on-scroll').forEach((el) => {
        observer.observe(el);
    });
}

// Initialize on page load too (for non-Arizona environments)
document.addEventListener('DOMContentLoaded', initScrollAnimations);
