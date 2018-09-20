open Reprocessing;

let stepSize = 10;
let width = 500;
let height = 500;

module Direction = {
  type t =
    | Up
    | Down
    | Left
    | Right;
};

type status =
  | Running
  | GameOver
  | Pause;

type state = {
  snake: list((int, int)),
  direction: Direction.t,
  status,
  time: float,
  appleTime: float,
  apples: list((int, int)),
};

let getNewApple = _state => (Random.int(49) * 10, Random.int(49) * 10);

let setup = env => {
  Env.size(~width, ~height, env);
  {
    snake: [(50, 50), (50, 60), (50, 70)],
    direction: Direction.Down,
    status: Running,
    time: 0.0,
    appleTime: 0.0,
    apples: [],
  };
};

let getNewHead = (snake, direction) => {
  let (lastX, lastY) = List.hd(List.rev(snake));

  switch (direction) {
  | Direction.Up =>
    let newY = lastY - stepSize;
    if (newY < 0) {
      (lastX, height - stepSize);
    } else {
      (lastX, newY);
    };
  | Direction.Down =>
    let newY = lastY + stepSize;
    if (newY >= height) {
      (lastX, 0);
    } else {
      (lastX, newY);
    };
  | Direction.Left =>
    let newX = lastX - stepSize;
    if (newX < 0) {
      (width - stepSize, lastY);
    } else {
      (newX, lastY);
    };
  | Direction.Right =>
    let newX = lastX + stepSize;
    if (newX >= width) {
      (0, lastY);
    } else {
      (newX, lastY);
    };
  };
};

let moveSnake = (snake, direction) => {
  let tail = List.tl(snake);

  List.append(tail, [getNewHead(tail, direction)]);
};

let growSnake = (snake, direction) =>
  List.append(snake, [getNewHead(snake, direction)]);

let eat = (snake: list((int, int)), apples: list((int, int)), direction) => {
  let snakeHead = List.hd(List.rev(snake));
  let newApples =
    List.fold_right(
      (apple, acc) =>
        if (apple != snakeHead) {
          [apple, ...acc];
        } else {
          acc;
        },
      apples,
      [],
    );

  let newSnake =
    if (List.length(newApples) !== List.length(apples)) {
      growSnake(snake, direction);
    } else {
      snake;
    };

  (newSnake, newApples);
};

let draw = (state, env) => {
  let backgroundColor = switch(state.status) {
    | GameOver => Constants.red
    | _ => Constants.white
  };

  Draw.background(backgroundColor, env);
  Draw.fill(Constants.black, env);
  let deltaTime = Env.deltaTime(env) +. state.time;
  let deltaAppleTime = Env.deltaTime(env) +. state.appleTime;

  let newSnake =
    switch (deltaTime > 0.3, state.status) {
      | (_, GameOver) => state.snake;
      | (true, _) => moveSnake(state.snake, state.direction);
      | _ => state.snake;
    }

  let newTime =
    if (deltaTime > 0.3) {
      0.0;
    } else {
      deltaTime;
    };

  let newApples =
    if (deltaAppleTime > 10.0) {
      [getNewApple(state), ...state.apples];
    } else {
      state.apples;
    };

  let newAppleTime =
    if (deltaAppleTime > 10.0) {
      0.0;
    } else {
      deltaAppleTime;
    };

  let (newSnake, newApples) = eat(newSnake, newApples, state.direction);

  let reversedSnake = List.rev(newSnake);
  let snakeHead = List.hd(reversedSnake);
  let newStatus = if (List.mem(snakeHead, List.tl(reversedSnake))) {
    GameOver;
  } else {
    state.status;
  };

  let newState = {
    ...state,
    snake: newSnake,
    time: newTime,
    apples: newApples,
    appleTime: newAppleTime,
    status: newStatus
  };

  List.iter(
    pos => Draw.rect(~pos, ~width=10, ~height=10, env),
    newState.snake,
  );

  List.iter(
    pos => {
      Draw.fill(Constants.red, env);
      Draw.rect(~pos, ~width=10, ~height=10, env);
    },
    newState.apples,
  );

  newState;
};

let isOpossite = (currentDirection, keyPressed) =>
  switch (currentDirection, keyPressed) {
  | (Direction.Up, Events.Down)
  | (Direction.Down, Events.Up)
  | (Direction.Left, Events.Right)
  | (Direction.Right, Events.Left) => true
  | _ => false
  };

let keyPressed = (state, env) =>
  /* let (posX, posY) = state.position; */
  if (isOpossite(state.direction, Env.keyCode(env))) {
    state;
  } else {
    switch (Env.keyCode(env)) {
    | Up => {...state, direction: Direction.Up}
    | Down => {...state, direction: Direction.Down}
    | Left => {...state, direction: Direction.Left}
    | Right => {...state, direction: Direction.Right}
    | _ => state
    };
  };

run(~setup, ~draw, ~keyPressed, ());
