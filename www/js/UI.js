var UI = {}

UI.Element = function () {}

UI.Element.prototype =
{
    setClass: function (name)
    {
        this.dom.className = name
        return this
    },

    setStyle: function (style, array)
    {
        for (var i = 0; i < array.length; i ++)
            this.dom.style[style] = array[i]
    },

    setTextContent: function (value)
    {
        this.dom.textContent = value
        return this
    }
}

var properties = ['position', 'left', 'top', 'right', 'bottom', 'width', 'height', 'border', 'borderLeft',
'borderTop', 'borderRight', 'borderBottom', 'margin', 'marginLeft', 'marginTop', 'marginRight',
'marginBottom', 'padding', 'paddingLeft', 'paddingTop', 'paddingRight', 'paddingBottom', 'color',
'backgroundColor', 'fontSize', 'fontWeight', 'display', 'overflow', 'cursor']

properties.forEach (function (property)
{
    var method = 'set' + property.substr (0, 1).toUpperCase () + property.substr (1, property.length)
    UI.Element.prototype[method] = function () { this.setStyle (property, arguments); return this; }
})

var events = ['MouseOver', 'MouseOut', 'Click']

events.forEach (function (e)
{
	var method = 'on' + e
	UI.Element.prototype[method] = function (callback) { this.dom.addEventListener (e.toLowerCase (), callback, false); return this; }
})

UI.Panel = function (props)
{
    UI.Element.call (this)

    var dom = document.createElement ('div')
    if (props && props.clazz) dom.className = props.clazz
    else                      dom.className = 'Panel'

    if (props && props.text) dom.textContent = props.text

    this.dom = dom

    return this
}

UI.Panel.prototype = Object.create (UI.Element.prototype)

UI.Panel.prototype.add = function ()
{
    for (var i = 0; i < arguments.length; i ++)
        this.dom.appendChild (arguments[i].dom)

    return this
}
