var options = 
  {  enableHighAccuracy:true, timeout: 5000, maximumAge: 0}; 
function success(pos) {
  var crd = pos.coords;
}
function error(err) {
  console.warn('ERROR(' + err.code + '): ' + err.message); } 
function getLocation() {
  navigator.geolocation.getCurrentPosition(success, error, options); }
function populateTextBoxes() {
  document.getElementById('latitudeText').value = position.coords.latitude;
  document.getElementById('longitudeText').value = position.coords.longitude;
}