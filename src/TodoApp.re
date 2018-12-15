/* type and variable names must be lowercase
    variant (enum) and module names must start uppercase */

/* Action items are ways to update the state */
type action = 
  | AddItem(string)
  | ToggleItem(int);

/* basically a typedef struct */
type item = {
  title: string,
  completed: bool,
  id: int
};

type state = {
  items: list(item) /* list is a built in type. We want a list of items */
};

let str = ReasonReact.stringToElement;


/* React component to list out the todo items */
module TodoItem = {
  let component = ReasonReact.statelessComponent("TodoItem");

  /* ~item means this function takes a variable known as item interally and externally */
  let make = (~item, ~onToggle, children) =>{
   ...component,
    render: (_) =>
      <div className="item" onClick=((_evt) => onToggle())>
        <input
          _type="checkbox"  /* article is wrong here */
          checked=Js.Boolean.to_js_boolean(item.completed) /* jesus ... fixed in BS3.0 apparently https://reasonml.github.io/docs/en/boolean */
          /* TODO make interactive */
        />
        (str(item.title))
        <p>(str(string_of_int((item.id))))</p>
      </div>
  }
};

/* This is setting up the equivalent of evt.target.value, which pulls the value of the thing that triggered the event */
let valueFromEvent = (evt) : string =>(
    evt
    |> ReactEventRe.Form.target
    |> ReactDOMRe.domElementToObj
)##value;   /* get the target of the event, turn it into a generic object, magic accessor syntax the value of it */

module Input = {
    type state = string;
    let component = ReasonReact.reducerComponent("Input");
    let make = (~onSubmit, _) => {
        ...component, 
        initialState: () => "",
        reducer: (newText, _text) => ReasonReact.Update(newText),
        render: ({state: text, send}) =>

            <input
                value=text
                _type="text"
                placeholder="write something"
                onChange=((evt) => send(valueFromEvent(evt)))   /* update the state with the value of the text box */
                onKeyDown=((evt) => /* enter key handler */
                if(ReactEventRe.Keyboard.key(evt) == "Enter"){
                    onSubmit(text);
                    send("")
                })
                />
    }; 
}; 


/* bind a couple of names so things are less gross */
let component = ReasonReact.reducerComponent("TodoApp");
let lastId = ref(0);  /* ref makes this variable mutable (able to be mutated - changed from elsewhere) */

let newItem = (text) =>{
  lastId := lastId^ + 1;  /* use := to assign to a ref, ^ to dereference (get the value of) a ref */
  {title: text, completed: true, id: lastId^}
  };

let make = _children => {
  ...component, /* returning a ReasonReact.reducerComponent */
  /* ... is the spread operator, lets you pull out and override methods (in this case, the render method) */
 
/* set some initial state */
initialState:() => {
  items: [{title: "write down some todo items", completed: false, id:0}]  /* the type will be inferred! Nuts! */
},

/* make a reducer */
reducer: (action, {items}) => 
  switch action{
    | AddItem(text) => ReasonReact.Update({items: [newItem(text), ...items]})   /* When the AddItem action is received, append the newItem() to the state.items */
    | ToggleItem(id) =>
      let items = List.map(
        (item) =>
          item.id === id?
          {...item, completed: ! item.completed} : item,
          items
      );
      ReasonReact.Update({items: items})
  },

 /* override the render method */
 
 render: ({state: {items}, send}) => {/* standard react, call to render returns some HTML (JSX or its reason version) */
    /* self is the argument */
    /* use self.state to access state */
    /* use self.send to update state */
    let numItems = List.length(items);
    <div className="app">
      <div className="title"> (str("What to do")) 
        <Input onSubmit=((text) => send(AddItem(text))) />
        /* <button
          onClick=((_evt) => send(AddItem))>
          (str("Add something"))
        </button> */
        </div>
 <div className="items">
        (ReasonReact.arrayToElement(Array.of_list(      /* example uses ReasonReact.array */
          List.map(
            (item) => <TodoItem
              key=(string_of_int(item.id))
              onToggle=(() => send(ToggleItem(item.id)))
              item
            />, items
          )
        )))
      </div>
      <div className="footer">
        (str(string_of_int(numItems) ++ " items"))
      </div>
    </div>
 }
};


