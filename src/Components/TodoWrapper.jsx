import { useState } from 'react';
import { v4 as uuidv4 } from 'uuid';
import { TodoItem } from './TodoItem';
import { AddTodoForm } from './AddTodoForm';
import { EditTodoForm } from './EditTodoForm';

export const TodoWrapper = () => {
    const [todos, setTodos] = useState([]);

    const addTodo = todo => {
        setTodos([...todos, 
            {
                id: uuidv4(), 
                task: todo,
                completed: false,
                isEditing: false,
            }
        ]);
    }

    const toggleComplete = id => {
        setTodos(todos.map(
            todo => todo.id === id ? {...todo, completed: !todo.completed} : todo)
        );
    }

    const deleteTodo = id => {
        setTodos(todos.filter(
            todo => todo.id !== id)
        );
    }

    const editTodo = id => {
        setTodos(todos.map(
            todo => todo.id === id ? {...todo, isEditing: !todo.isEditing} : todo)
        );
    }

    const editTask = (id, updatedTask) => {
        setTodos(todos.map(
            todo => todo.id === id ? {...todo, task: updatedTask, isEditing: !todo.isEditing} : todo)
        );
    }

    return (
        <div className="TodoWrapper">
            {/* Title */}
            <h1 className="page-title">TODO</h1>

            {/* Add new task input field */}
            <AddTodoForm addTodo={addTodo}/>

            {/* List of added tasks */}
            {todos.map((todo, index) => (
                !todo.isEditing ? (
                    <TodoItem 
                        task={todo} 
                        key={index} 
                        toggleComplete={toggleComplete}
                        deleteTodo={deleteTodo} 
                        editTodo={editTodo}
                    /> 
                ) : (
                    <EditTodoForm 
                        editTodo={editTask} 
                        task={todo}
                    /> 
                )
            ))}
        </div>
    )
}