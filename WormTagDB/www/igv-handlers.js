// www/igv-handlers.js
window.igvBrowser = null;

document.addEventListener('DOMContentLoaded', () => {
  const igvDiv = document.getElementById('igv-container');
  if (!igvDiv) return;

  igv.createBrowser(igvDiv, { genome: 'ce11', locus: 'chrI:12,301,000-12,304,000' })
    .then(b => {
      window.igvBrowser = b;
      if (window.Shiny) Shiny.setInputValue('igv_ready', Math.random());
    });
});

// Jump to a locus
Shiny.addCustomMessageHandler('igv-goto', locus => {
  if (window.igvBrowser) window.igvBrowser.search(locus);
});

// Replace or add a named annotation track using inline features
// Replace or add a named annotation track using inline features
Shiny.addCustomMessageHandler('igv-load-track', payload => {
  if (!window.igvBrowser) return;
  // remove an existing track with same name (avoid duplicates)
  const existing = window.igvBrowser.trackViews.find(tv => tv.track?.name === payload.name);
  if (existing) window.igvBrowser.removeTrack(existing.track);
  
  // helper: normalize itemRgb if present
  const rgbFromItemRgb = v =>
    !v ? null :
    Array.isArray(v) ? `rgb(${v.join(',')})` :
    (/^\d+,\d+,\d+$/.test(v) ? `rgb(${v})` : v);
  
  window.igvBrowser.loadTrack({
    name: payload.name,
    type: 'annotation',
    format: 'bed',
    displayMode: 'EXPANDED',
    indexed: false,
    sourceType: 'file',
    features: payload.features,
    // **Honor per-feature color first, AND set altColor for minus strand**
    color: f => f?.color || rgbFromItemRgb(f?.itemRgb) || (payload.color || '#334EAA'),
    altColor: f => f?.color || rgbFromItemRgb(f?.itemRgb) || (payload.color || '#334EAA'),
    useScore: false,
    colorBy: undefined,
    height: 40,
    featureHeight: 12,
    visibilityWindow: 200000,
    showAllFeatureLabels: true
  });
});