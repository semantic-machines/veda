function guid() {
  function s4() {
    return Math.floor((1 + Math.random()) * 0x10000)
               .toString(16)
               .substring(1);
  }
  return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
         s4() + '-' + s4() + s4() + s4();
}

function compare (a, b) {
	if (typeof a === "function") return a.toString() === b.toString();
	else if (typeof a != "object" || typeof b != "object") return a === b;
	if (Object.keys(a).length != Object.keys(b).length) return false;
	var result = true;
	for (var key in a) {
		result &= compare(a[key], b[key]);
		if (!result) return false;
	}
	return result;
}

function sleep(usec) {
	var endtime= new Date().getTime() + usec;
    while (new Date().getTime() < endtime);
}