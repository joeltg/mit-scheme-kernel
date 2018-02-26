define("mywidget", ["@jupyter-widgets/base"], function(widgets) {
	const MyWidgetView = widgets.DOMWidgetView.extend({
		render() {
			MyWidgetView.__super__.render.apply(this, arguments)
			this._count_changed()
			this.listenTo(this.model, "change:count", this._count_changed, this)
		},

		_count_changed() {
			var old_value = this.model.previous("count")
			var new_value = this.model.get("count")
			this.el.textContent = String(old_value) + " -> " + String(new_value)
		},
	})

	return { MyWidgetView }
})
