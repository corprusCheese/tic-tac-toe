<script>
	import axios from "axios";

	const devUrl = "https://peaceful-depths-92861.herokuapp.com/"

	const instance = axios.create({
  		baseURL: devUrl
	});

	let result = ""

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
				url: "/board",
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

					result = getResult(res.data.result)
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
		instance.get('/board/clear').then(res => {
			const boardValues = new Map(Object.entries(res.data.board))
			getBoard(boardValues)
			document.getElementById("reset").addEventListener("click", resetHandler())
			result = ""
			setTimeout(callback.call(this), 1000)
		})
	}

	function getResult(dataResult) {
		let res = ""
		switch (dataResult) {
			case "none": break;
			case "circle wins": res = "Circle Wins!"; break;
			case "cross wins": res = "Cross Wins!"; break;
		}

		return res
	}

	function resetHandler() {
		return () => {
			instance.get("/board/clear").then(res => {
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
