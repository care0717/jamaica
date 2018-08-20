function getAnswers(answer, dices, onSuccess, onError)
{
  let url = "/answers/" + answer
  dices.forEach(element => {
    url += "/" + element
  }); 
  $.ajax(
    { url: url
    , success: onSuccess
    , error: onError
    , type: 'GET'
    }
  );
}
function search(){
  let dices = []
  $('input[name^=dice]').each(function(){
    dices.push($(this).val()) 
  });
  let answer = $('input[name=answer]').val()
  getAnswers(answer, dices, function(answers){
    var table = $("table#form");
    answers.forEach(function(answer){
      var title = $("<td />").text(answer.answer);
      var dice = new Array(5)
      dice[0] = $("<td />").text(answer.dice1);
      dice[1] = $("<td />").text(answer.dice2);
      dice[2] = $("<td />").text(answer.dice3);
      dice[3] = $("<td />").text(answer.dice4);
      dice[4] = $("<td />").text(answer.dice5);
      var tr = $("<tr />").append(title)
      dice.forEach( d => {
        tr = tr.append(d)
      })
      table.append(tr);
    });
  });
}
$(function(){
  $(".button").click(search);
})
