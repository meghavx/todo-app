import React, { useState } from 'react';

export const EditTodoForm = ({editTodo, task}) => {
    const [value, setValue] = useState(task.task);

    const handleSubmit = e => {
        e.preventDefault();
        editTodo(task.id, value);
    }

    return (
        <form className="TodoForm" onSubmit={handleSubmit}>
            <input type="text" value={value}className="todo-input edit-input" placeholder="Update task..." onChange={e => setValue(e.target.value)}/>
            <button type="submit" className="todo-edit-btn">
                Update
            </button>
        </form>
    )
}

