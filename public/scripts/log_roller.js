var g_requests = new Array();

Event.observe(window, 'load', page_load);
Event.observe(document, 'keypress',  key_pressed);

function key_pressed(event) { 
    if(event.keyCode == 13) { 
		refresh();
	} else if (event.keyCode == 102) {
	    toggle_filters();
	} else if (event.keyCode == 91) {
	    prev_tab();
	} else if (event.keyCode == 93) {
        next_tab();
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
            if(logs.responseText.match(/^\s+$/) != null) {
                $('log_frame').innerHTML = 'no logs met your search criteria';
            }
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

function toggle_filters() {
    if($('filter_bar_open').style.display == 'none') {
        $('filter_bar_open').style.display = 'block';
        $('filter_bar_closed').style.display = 'none';
    } else {
        $('filter_bar_open').style.display = 'none';
        $('filter_bar_closed').style.display = 'block';
    }
}

function prev_tab() {
    var tabs = document.getElementsByName('tab');
    var is_next = false;
    for(var i=tabs.length-1; i>=0; i--) {
        if(is_next) {
            switch_server(tabs[i].childNodes[0].innerHTML);
            i = 0;
        } else if(tabs[i].className == 'active_tab') {
            if(i == 0) {
                switch_server(tabs[tabs.length-1].childNodes[0].innerHTML);
            } else {
                is_next = true;
            }
        } else if(i == 0) {
            switch_server(tabs[tabs.length-1].childNodes[0].innerHTML);
        }
    }
}

function next_tab() {
    var tabs = document.getElementsByName('tab');
    var is_next = false;
    for(var i=0; i<tabs.length; i++) {
        if(is_next) {
            switch_server(tabs[i].childNodes[0].innerHTML);
            i = tabs.length;
        } else if(tabs[i].className == 'active_tab') {
            if(i == (tabs.length-1)) {
                switch_server(tabs[0].childNodes[0].innerHTML);
            } else {
                is_next = true;
            }
        } else if(i == (tabs.length-1)) {
            switch_server(tabs[0].childNodes[0].innerHTML);
        }
    }    
}