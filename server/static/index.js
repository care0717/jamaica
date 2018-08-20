function getAnswers(onSuccess, onError)
{
  $.ajax(
    { url: '/answers/11/1/1/1/1/5'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
