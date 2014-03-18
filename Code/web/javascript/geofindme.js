    function geoFindMe() {
      var output = document.getElementById('out');
      if (!navigator.geolocation){
        output.innerHTML = '<p>Geolocation is not supported by your browser</p>';
        return;
      }
      function success(position) {
        var latitude  = position.coords.latitude;
        var longitude = position.coords.longitude;
    
        document.getElementById('latitudeText').value = latitude;
        document.getElementById('longitudeText').value = longitude;
    
        var img = new Image();
        img.src = 'http://maps.googleapis.com/maps/api/staticmap?center=' + latitude + ',' + longitude + '&zoom=15&size=500x500&markers=color:blue%7Clabel:S%7C' + latitude + ',' + longitude + '&sensor=false';
    
        output.appendChild(img);
      }
      function error() {
        output.innerHTML = 'Unable to retrieve your location';
      }
      navigator.geolocation.getCurrentPosition(success, error);
    }
