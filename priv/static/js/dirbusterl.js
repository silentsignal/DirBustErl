function start_bust(e) {
    e.preventDefault();
	alert("TODO: bust");
}

function load_sessions() {
	$.getJSON("/bust", null, load_sessions_data);
}

function session_detail_clicked(event) {
	var tbody = $("#detailsModal .detailsParams tbody").empty();
	var config = event.data.config;
	var tr = document.createElement("tr");
	var key = document.createElement("td");
	var value = document.createElement("td");
	key.appendChild(document.createTextNode("Recursively bust directories"));
	value.appendChild(document.createTextNode(config.follow_dirs ? "enabled" : "disabled"));
	tr.appendChild(key);
	tr.appendChild(value);
	tbody.append(tr);
	$('#detailsModal').modal({});
}

function load_sessions_data(sessions) {
	var tbody = $("#sessions tbody").empty();
	$(sessions).each(function (n, session) {
		var tr = document.createElement("tr");
		var id = document.createElement("td");
		var url = document.createElement("td");
		var a = document.createElement("a");
		var st = document.createElement("td");
		var details = document.createElement("td");
		var button = document.createElement("a");
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
		button.className = "btn btn-info btn-xs";
		button.appendChild(document.createTextNode("Details"));
		details.appendChild(button);
		tr.appendChild(details);
		tbody.append(tr);
		$(button).on("click", null, session, session_detail_clicked);
	});
}

$(function() {
    $("#bust").submit(start_bust);
	load_sessions();
});
