<script>
	import axios from "axios";

	function getBoard(board) {
		let html = ""
		board.forEach((row, i) => {
			row = new Map(Object.entries(row))
			let rowWrap = ""
			row.forEach((item, j) => {
				rowWrap += getItem(item, i, j)
			})
			html += rowWrapper(rowWrap)
		})
		return html
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
		return "<div class='ceil' on:click={console.log(rowIndex, index)} data-i='"+rowIndex+"' data-j='"+index+"'>"+html+"</div>"
	}

	function rowWrapper(html) {
		return "<div class='row-wrapper'>"+html+"</div>"
	}

	let htmlBoard = ""
	let result = ""
	axios.get('/board').then(res => {
		const board = new Map(Object.entries(res.data.board))
		htmlBoard = getBoard(board)
	})

</script>

<main>
	<div class="wrapper">
		<div class="board" >{@html htmlBoard}</div>
	</div>
	<div class="reset"> Заново </div>
	<div class="result"> { result } </div>
</main>
