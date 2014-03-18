document.getElementById('nearbyStopsButton').onclick = function () {
       var lat   = document.getElementById('latitudeText').value;
       var lon   = document.getElementById('longitudeText').value;
       var units = document.getElementsByName('units');
       var i;
       for (i=0;i<units.length;i++)
         {
         if (units[i].checked)
           {
           units = units[i].value;
           }
         }
       var radius = document.getElementById('radius').value;
       location.href = 'http://transittracker.dnsdynamic.com:8000/stopsNearby/' + 
                        lat + ',' + 
                        lon + ',' + 
                        units + ',' + 
                        radius + '/';
     };