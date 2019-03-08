// define("mywidget", ["@jupyter-widgets/base"], function(widgets) {
// 	console.log("defining widgets or something")
// 	const MyWidgetView = widgets.DOMWidgetView.extend({
// 		render() {
// 			MyWidgetView.__super__.render.apply(this, arguments)
// 			this._count_changed()
// 			this.listenTo(this.model, "change:count", this._count_changed, this)
// 		},

// 		_count_changed() {
// 			var old_value = this.model.previous("count")
// 			var new_value = this.model.get("count")
// 			this.el.textContent = String(old_value) + " -> " + String(new_value)
// 		},
// 	})

// 	return {
// 		MyWidgetView,
// 		onload() {
// 			console.log("LOADED THIS THING THING")
// 		},
// 	}
// })

// module.exports = {
// 	onload() {
// 		console.log("MAYBE THIS WILL WORK", arguments)
// 	},
// }

// define(function(require, config, options) {
// 	return {
// 		onload: function() {
// 			console.info(
// 				"Kernel specific javascript loaded FJSDKLF JSDKF JKLSD FJKLSD JFLKS"
// 			)
// 		},
// 	}
// })

function handle_kernel(Jupyter, kernel) {
	if (kernel.comm_manager && kernel.widget_manager === undefined) {
		// Clear any old widget manager
		if (Jupyter.WidgetManager) {
			Jupyter.WidgetManager._managers[0].clear_state()
		}

		kernel.widget_manager = new Jupyter.WidgetManager(
			kernel.comm_manager,
			Jupyter.notebook
		)
	}
}

define(["base/js/namespace", "base/js/events", "notebook/js/outputarea"], (
	Jupyter,
	events,
	outputarea
) => ({
	onload() {
		if (Jupyter.notebook && Jupyter.notebook.kernel) {
			handle_kernel(Jupyter, Jupyter.notebook.kernel)
		} else {
			const handle = "kernel_created.Kernel kernel_created.Session"
			events.on(handle, (event, data) => handle_kernel(Jupyter, data.kernel))
		}
		const handle =
			"kernel_killed.Session kernel_killed.Kernel kernel_restarting.Kernel"
		events.on(handle, (event, data) => {
			const { kernel } = data
			if (kernel && kernel.widget_manager) {
				kernel.widget_manager.disconnect()
			}
		})
		console.log("LOADED THE SHIT", stuff)
	},
}))
