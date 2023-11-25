import { useState } from 'react'

export const AddTodoForm = ({addTodo}) => {
    const [value, setValue] = useState("");

    const handleSubmit = e => {
        e.preventDefault();
        addTodo(value);
        setValue("");
    }

    return (
        <form className="AddTodoForm" onSubmit={handleSubmit}>
            <input 
                type="text" value={value}   
                className="add-input"   
                placeholder="Enter your task here!"   
                onChange={e => setValue(e.target.value)}
            />
            <button type="submit" className="add-btn">
                Add
            </button>
        </form>
    )
}
