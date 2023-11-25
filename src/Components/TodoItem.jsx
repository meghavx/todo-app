import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faPenToSquare } from '@fortawesome/free-regular-svg-icons';
import { faTrash } from '@fortawesome/free-solid-svg-icons';

export const TodoItem = ({task, toggleComplete, deleteTodo, editTodo}) => {
    return (
        <div className="TodoItem">
            
            {/* Task name */}
            <p className={`${task.completed ? "completed " : ""}` + "task-name"} onClick={() => toggleComplete(task.id)}>
                {task.task}
            </p>

            {/* Task control icons: edit and delete */}
            <div className="control-icons">
                <FontAwesomeIcon 
                    icon={faPenToSquare} 
                    className="icon" 
                    onClick={() => editTodo(task.id)}
                />
                <FontAwesomeIcon 
                    icon={faTrash} 
                    className="icon" 
                    onClick={() => deleteTodo(task.id)}
                />
            </div>
        </div>
    )
}
