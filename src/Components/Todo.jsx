import { faPenToSquare } from '@fortawesome/free-regular-svg-icons'
import { faTrash } from '@fortawesome/free-solid-svg-icons'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import React from 'react'

export const Todo = ({task, toggleComplete, deleteTodo, editTodo}) => {
  return (
    <div className="Todo">
      <p className={`${task.completed ? "completed " : ""}` + "todo-item"} onClick={() => toggleComplete(task.id)}>
        {task.task}
      </p>
      <div className="todo-control-icons-div">
        <FontAwesomeIcon icon={faPenToSquare} className="todo-control-icons" onClick={() => editTodo(task.id)}/>
        <FontAwesomeIcon icon={faTrash} className="todo-control-icons" onClick={() => deleteTodo(task.id)}/>
      </div>
    </div>
  )
}
