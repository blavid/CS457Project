document.getElementById('arrivalsButton').onclick = function () {
  location.href = 'http://transittracker.dnsdynamic.com:8000/arrivals/' + 
         document.getElementById('arrivalsText').value + '/';
}
