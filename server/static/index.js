function getAnswers(answer, dices, onSuccess, onError) {
  let url = "/answers/" + answer
  dices.forEach(element => {
    url += "/" + element
  });
  $.ajax(
    {
      url: url
      , success: onSuccess
      , error: onError
      , type: 'GET'
    }
  );
}

function getProblem(level, onSuccess, onError) {
  let url = "/problem/" + level
  $.ajax(
    {
      url: url
      , success: onSuccess
      , error: onError
      , type: 'GET'
    }
  );
}

function printSolution(solution_example) {
  let solution = solution_example.slice(1, 18).split(",").reverse()
  let pair = [[" (", " )"], [" [", " ]"], [" {", " }"]]
  let pair_index = 0
  let index = 0
  while (solution.length > 1) {
    if (solution[index].length == 1 && solution[index].match(/[+-/*]/) != null) {
      let exp = pair[pair_index][0] + " " + solution[index - 1] + " " + solution[index] + " " + solution[index - 2] + pair[pair_index][1]
      solution[index] = exp
      solution.splice(index - 2, 2)
      index -= 2
      pair_index = (pair_index + 1) % 3
    }

    index += 1
  }
  let size = solution[0].length
  return solution[0].slice(2, size - 1)
}

function search() {
  let dices = []
  $('input[name^=dice]').each(function () {
    dices.push($(this).val())
  });
  dices.sort()
  let answer = $('input[name=answer]').val()
  getAnswers(answer, dices, function (answers) {
    if (answers.length <= 0) {
      $("#dummy").html('<strong>解けません</strong>')
    } else {
      answers.forEach(function (answer) {
        $("#dummy").html('<p id="result"><strong>答えが存在します' + "(解の数: " + answer.solution_number + ')</strong><p>')
        let result = "答え: " + printSolution(answer.solution_example)
        $("#result").append('<p><a class="btn tooltipped" data-position="bottom" data-tooltip="' + result + '">答えを表示しますか？</a></p>')
        $('.tooltipped').tooltip();
      });
    }
  });
  return false
}

function problem() {
  let level = $('[name=level] option:selected').val();
  getProblem(level, (problem) => {
    $('#answer').val(problem.answer)
    $('#dice1').val(problem.dice1)
    $('#dice2').val(problem.dice2)
    $('#dice3').val(problem.dice3)
    $('#dice4').val(problem.dice4)
    $('#dice5').val(problem.dice5)
  });
  return false;
}

$(function () {
  $('select').formSelect();
  $("#answerForm").submit(search);
  $("#problemForm").submit(problem);
})
