namespace FScript.Runtime

open FScript.Language

module TaskExterns =
    let spawn = TaskFunctions.spawn
    let awaitTask = TaskFunctions.awaitTask
    let await_task = awaitTask
    let all = [spawn; awaitTask]
