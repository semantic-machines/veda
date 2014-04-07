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
	if (Object.keys(a).length != Object.keys(b).length) return false;
	var result = true;
	for (var key in a) {
		if (typeof a[key] === "object") result &= compare(a[key], b[key]);
		else result &= a[key] == b[key] ? true : false;
	}
	return result;
}