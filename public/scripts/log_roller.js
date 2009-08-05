var g_requests = new Array();

Event.observe(window, 'load', page_load);
Event.observe(document, 'keypress',  key_pressed);

function key_pressed(event) { 
	if(event.keyCode == 13) { 
		refresh();
	} 
}

function page_load() {
	new Ajax.Request("/servers/" + $('server').value, {
		method: 'get',
		onSuccess: function(tabs) {
			$('tab_bar').innerHTML = tabs.responseText;
		}
	});
	
	new Ajax.Request("/nodes/" + $('server').value, {
		method: 'get',
		onSuccess: function(nodes) {
			$('sl_node').innerHTML = nodes.responseText;
		}
	});
	
	refresh();
}

function refresh() {
	$('log_frame').innerHTML = '';
	
	new Ajax.Request("/logs", {
		method: 'post',
		parameters: {
			server: $('server').value,
			type: $('sl_type').value, 
			node: $('sl_node').value,
			grep: $('grep').value,
			max: $('max').value
		},
		onCreate: function(request) {
			for(var i=0; i<g_requests.length; i++) {
				try { g_requests[i].transport.abort(); } catch(e) {}
				$('int_log_frame').innerHTML = '';
			}
			g_requests.push(request);
			$('ajax-loader').style.display = 'inline';
		},
		onInteractive: function(logs) {
			$('int_log_frame').innerHTML = logs.responseText;
		},
		onComplete: function(logs) {
			$('log_frame').innerHTML += logs.responseText;
			$('int_log_frame').innerHTML = '';
			$('ajax-loader').style.display = 'none';
		}
	});
}

function switch_server(server_name) {
	$('server').value = server_name;
	$('sl_type').selectedIndex = -1;
	$('grep').value = '';
	$('max').value = '';
	page_load();
}

function open_filters() {
	document.getElementById('filter_bar_open').style.display = "block";
	document.getElementById('filter_bar_closed').style.display = "none";
}

function close_filters() {
	document.getElementById('filter_bar_open').style.display = "none";
	document.getElementById('filter_bar_closed').style.display = "block";
}

function getElementWithFocus() {
	var hf;
	if ( document.activeElement ) 
		hf = document.activeElement;
	else 
		hf = document.focusNode;
	return hf;
}