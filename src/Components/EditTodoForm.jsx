import React, { useState } from 'react';

export const EditTodoForm = ({editTodo, task}) => {
    const [value, setValue] = useState(task.task);

    const handleSubmit = e => {
        e.preventDefault();
        editTodo(task.id, value);
    }

    return (
        <form className="TodoForm" onSubmit={handleSubmit}>
            <input 
                type="text" value={value} 
                className="edit-input" 
                placeholder="Update task..." 
                onChange={e => setValue(e.target.value)}
            />
            <button type="submit" className="update-btn">
                Update
            </button>
        </form>
    )
}

