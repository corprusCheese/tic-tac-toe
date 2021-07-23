<script>
	import axios from "axios";

	const devUrl = "https://peaceful-depths-92861.herokuapp.com/"

	const instance = axios.create({
  		baseURL: devUrl
	});

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
		cell.addEventListener("click", clickHandler(rowIndex, index, null));

		cell.innerHTML = html

		return cell
	}

	function clickHandler(i, j) {
		return () => {
			console.log(i+", "+j)
		}
	}

	let result = ""
	instance.get('/board').then(res => {
		const boardValues = new Map(Object.entries(res.data.board))
		getBoard(boardValues)
	})

</script>

<main>
	<div class="wrapper">
		<div id="board" class="board"></div>
	</div>
	<div class="reset"> Заново </div>
	<div class="result"> { result } </div>
</main>
