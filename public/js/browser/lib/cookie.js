// JavaScript Document for b2

// возвращает cookie если есть или undefined
function getCookie(name) {
	var matches = document.cookie.match(new RegExp(
	  "(?:^|; )" + name.replace(/([\.$?*|{}\(\)\[\]\\\/\+^])/g, '\\$1') + "=([^;]*)"
	))
	return matches ? decodeURIComponent(matches[1]) : undefined 
}

/* уcтанавливает cookie
name
    название cookie
value
    значение cookie (строка)
props
    Объект с дополнительными свойствами для установки cookie:

    expires
        Время истечения cookie. Интерпретируется по-разному, в зависимости от типа:

            Если число - количество секунд до истечения.
            Если объект типа Date - точная дата истечения.
            Если expires в прошлом, то cookie будет удалено.
            Если expires отсутствует или равно 0, то cookie будет установлено как сессионное и исчезнет при закрытии браузера.

    path
        Путь для cookie.
    domain
        Домен для cookie.
    secure
        Пересылать cookie только по защищенному соединению.
*/

function setCookie(name, value, props) {
	props = props || {}
	var exp = props.expires
	if (typeof exp == "number" && exp) {
		var d = new Date()
		d.setTime(d.getTime() + exp*1000)
		exp = props.expires = d
	}
	if(exp && exp.toUTCString) { props.expires = exp.toUTCString() }

	value = encodeURIComponent(value)
	var updatedCookie = name + "=" + value
	for(var propName in props){
		updatedCookie += "; " + propName
		var propValue = props[propName]
		if(propValue !== true){ updatedCookie += "=" + propValue }
	}
	document.cookie = updatedCookie
}

// удаляет cookie
function delCookie(name) {
	setCookie(name, null, { expires: -1 })
}
