namespace FScript.Runtime

open FScript.Language

module TaskExterns =
    let spawn = TaskFunctions.spawn
    let awaitTask = TaskFunctions.awaitTask
    let await_task = awaitTask
    let delay = TaskFunctions.delay
    let fromResult = TaskFunctions.fromResult
    let continueWith = TaskFunctions.continueWith
    let whenAll = TaskFunctions.whenAll
    let whenAny = TaskFunctions.whenAny
    let awaitAll = TaskFunctions.awaitAll
    let all = TaskFunctions.all
