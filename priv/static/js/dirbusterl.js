function start_bust(e) {
    e.preventDefault();
	alert("TODO: bust");
}

function load_sessions() {
	$.ajax({
		url: "/bust",
		dataType: "json",
		success: load_sessions_data
	});
}

function load_sessions_data(sessions) {
	var tbody = $("#sessions tbody")
	$(sessions).each(function (n, session) {
		var tr = document.createElement("tr");
		var id = document.createElement("td");
		var url = document.createElement("td");
		var a = document.createElement("a");
		var st = document.createElement("td");
		id.className = "bust_id";
		id.appendChild(document.createTextNode(session.id));
		a.appendChild(document.createTextNode(session.url));
		a.href = session.url;
		url.appendChild(a);
		tr.appendChild(id);
		tr.appendChild(url);
		switch (session.status) {
			case "broken":
				tr.className = "danger";
				st.appendChild(document.createTextNode("internal error"));
				break;
			case "running":
				tr.className = "active";
				st.appendChild(document.createTextNode("running"));
				break;
			case "finished":
				tr.className = "success";
				st.appendChild(document.createTextNode("finished"));
				break;
			case "not_started":
				tr.className = "warning";
				st.appendChild(document.createTextNode("waiting to start"));
				break;
		}
		tr.appendChild(st);
		tbody.append(tr);
	});
}

$(function() {
    $("#bust").submit(start_bust);
	load_sessions();
});
