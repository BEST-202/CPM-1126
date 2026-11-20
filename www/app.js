// Navigation latérale -> onglets Shiny
$(document).on('click', '.nav-link-x', function (e) {
  e.preventDefault();
  var tab = $(this).data('tab');
  $('.nav-link-x').removeClass('active');
  $(this).addClass('active');
  document.body.classList.remove('nav-open');
  Shiny.setInputValue('nav', tab, { priority: 'event' });
  window.scrollTo({ top: 0, behavior: 'smooth' });
});

// Redimensionne les graphiques plotly quand un onglet devient visible
$(document).on('shown.bs.tab', function () {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 50);
});
Shiny.addCustomMessageHandler('resize', function (msg) {
  setTimeout(function () { window.dispatchEvent(new Event('resize')); }, 80);
});
