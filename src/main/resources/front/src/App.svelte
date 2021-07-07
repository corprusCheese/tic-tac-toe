<script>
	import axios from "axios";

	axios.get('/board').then(res => {
		res = res.data
		res['board'].forEach(item => {
			console.log(item)
		})
	})
</script>

<main>
	<div class="board"></div>
</main>
