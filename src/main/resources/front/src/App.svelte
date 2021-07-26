<script>
	import axios from "axios";
	import Swal from 'sweetalert2'

	const devUrl = "" // "https://peaceful-depths-92861.herokuapp.com/"

	const instance = axios.create(/*{
  		baseURL: devUrl
	}*/);

	let result = ""
	let boardId = ""

	function rowWrapper() {
		let row = document.createElement("div")
		row.setAttribute("class", 'row-wrapper')

		return row
	}

	function getBoard(boardValues) {
		let board = document.getElementById("board")
		boardValues.forEach((row, i) => {
			let rowWrap = rowWrapper()
			let rowEntries = new Map(Object.entries(row))
			rowEntries.forEach((item, j)=> {
				rowWrap.appendChild(getItem(item, i, j))
			})

			board.appendChild(rowWrap)
		})
	}

	function getItem(item, rowIndex, index) {
		let mark = ""
		switch (item) {
			case "cross":
				mark = "x"
				break
			case "circle":
				mark = "o"
				break
		}

		return cellWrapper(mark, rowIndex, index)
	}

	function cellWrapper(html, rowIndex, index) {
		let cell = document.createElement("div")
		cell.setAttribute("class", 'ceil')
		cell.setAttribute("data-i", rowIndex)
		cell.setAttribute("data-j", index)
		cell.addEventListener("click", clickHandler(cell, rowIndex, index));

		cell.innerHTML = html

		return cell
	}

	function clickHandler(cell, i, j) {
		return () => {
			instance({
				method: "post",
				url: "/board/"+boardId,
				data: {
					x: j,
					y: i
				}
			}).then((res) => {
				if (res.status === 200) {
					const boardValues = new Map(Object.entries(res.data.board))
					const rowValuesFound = new Map(Object.entries(boardValues.get(i.toString())))
					const cellValueFound = rowValuesFound.get(j.toString())

					cell.style.backgroundImage = setMark(cellValueFound)

					result = getResult(result, res.data.result)
				}
			})
		}
	}

	function setMark(cellValueFound) {
		let res = ""
		switch (cellValueFound) {
			case "none": res = ""; break;
			case "x": res = "url("+devUrl+"img/cross.png)"; break;
			case "o": res = "url("+devUrl+"img/circle.png)"; break;
		}

		return res
	}

	function setVisible(selector, visible) {
		document.querySelector(selector).style.display = visible ? 'block' : 'none';
	}

	function onReady(callback) {
		instance.get('/board/new').then(res => {
			const boardValues = new Map(Object.entries(res.data.board))
			getBoard(boardValues)
			document.getElementById("reset").addEventListener("click", resetHandler())
			result = ""
			boardId = res.data.id
			setTimeout(callback.call(this), 1000)
		})
	}

	function getResult(currentResult, dataResult) {
		let res = ""
		if (currentResult) {
			res = currentResult
		} else {
			switch (dataResult) {
				default:
					break;
				case "none":
					break;
				case "circle wins":
					res = "Circle Wins!";
					break;
				case "cross wins":
					res = "Cross Wins!";
					break;
			}

			showWindow(res)
		}

		return res
	}

	function showWindow(res) {
		if (res !== "")
			Swal.fire({
				title: res,
				text: "You can save result to server if you want",
				showCloseButton: true,
				showCancelButton: true,
				confirmButtonText: "Do it!",
				cancelButtonText: "No.."
			}).then((result) => {
				if (result.isConfirmed) {
					console.log("confirmed")
					instance.post("/board/"+boardId+"/save").then(result => {
						if (result.code === 200)
							console.log("success")
					})
				} else {
					console.log("cancelled")
				}
			})
	}

	function resetHandler() {
		return () => {
			instance.get("/board/"+boardId+"" + "/clear").then(res => {
				let board = document.getElementById("board")
				board.innerHTML = ""
				const boardValues = new Map(Object.entries(res.data.board))
				getBoard(boardValues)
				result = ""
			})
		}
	}

	onReady(function() {
		setVisible('.main', true);
		setVisible('#reset', true);
		setVisible('#loading', false);
	});
</script>

<main>
	<div class="main center">
		<div class="wrapper">
			<div id="board" class="board"></div>
		</div>
		<div id="reset" class="reset"> New Game </div>
		<div id="result" class="result"> { result } </div>
	</div>

	<div id="loading"></div>
</main>
