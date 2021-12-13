from typing import Callable, List, NamedTuple, Optional

import numpy
import numpy as np
from scipy import special
import matplotlib
import pytest
import requests
import tqdm
Array = np.ndarray


def ensure_array(array):
    return np.array(array, dtype="float32", copy=False)


class Dependency(NamedTuple):
    tensor: "Tensor"
    grad_fn: Callable[[Array], Array]


class Tensor:
    def __init__(
        self,
        data,
        depends_on: Optional[List[Dependency]] = None,
        requires_grad: bool = False,
    ) -> None:
        self.data = ensure_array(data)
        self.depends_on = depends_on or []
        self.requires_grad = requires_grad
        self.grad: Optional[Tensor] = None

    def __sub__(self, other) -> "Tensor":
        return sub(self, other)

    def __mul__(self, other) -> "Tensor":
        return mul(self, other)

    def __pow__(self, other) -> "Tensor":
        return power(self, other)

    def __matmul__(self, other) -> "Tensor":
        return matmul(self, other)

    def sum(self) -> "Tensor":
        return reduce_sum(self)

    def sigmoid(self) -> "Tensor":
        return sigmoid(self)

    def zero_grad_(self) -> None:
        # TODO
        #raise NotImplementedError
        self.grad = Tensor(np.zeros_like(self.data))

    def tolist(self):
        return self.data.tolist()

    @property
    def shape(self):   # [N, 1], [N, M]
        return self.data.shape

    def backward(self, grad: Optional["Tensor"] = None) -> None:
        if grad is None:
            if np.prod(self.data.shape) == 1:
                grad = Tensor(1)
            else:
                raise RuntimeError

        if self.grad is None:
            self.grad = Tensor(np.zeros_like(self.data))

        #raise NotImplementedError
        # TODO
        # hint: 4 lines of code
        self.grad.data += grad.data
        for dep in self.depends_on:
            g = dep.grad_fn(self.grad.data)
            dep.tensor.backward(Tensor(g))


def tensor(data, requires_grad: bool = False) -> Tensor:
    return Tensor(data, requires_grad=requires_grad)


def reduce_sum(inp: Tensor) -> Tensor:
    # TODO
    data = inp.data.sum()

    requires_grad = inp.requires_grad

    depends_on = []
    if requires_grad:

        def grad_fn(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            adj = np.ones_like(inp.data)

            return adj * grad
        depends_on.append(Dependency(tensor=inp, grad_fn=grad_fn))

    return Tensor(data, depends_on=depends_on, requires_grad=requires_grad)


def sub(left: Tensor, right: Tensor) -> Tensor:
    # TODO
    data = numpy.subtract(left.data, right.data)

    depends_on = []
    if left.requires_grad:

        def grad_fn_left(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            adj = np.ones_like(left.data)

            return adj * grad

        depends_on.append(Dependency(tensor=left, grad_fn=grad_fn_left))

    if right.requires_grad:

        def grad_fn_right(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            adj = np.negative(np.ones_like(right.data))

            return adj * grad

        depends_on.append(Dependency(tensor=right, grad_fn=grad_fn_right))

    requires_grad = left.requires_grad or right.requires_grad

    return Tensor(data=data, depends_on=depends_on, requires_grad=requires_grad)


def mul(left: Tensor, right: Tensor) -> Tensor:
    # TODO
    data = np.multiply(left.data, right.data)

    depends_on = []
    if left.requires_grad:

        def grad_fn_left(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            return right.data * grad

        depends_on.append(Dependency(tensor=left, grad_fn=grad_fn_left))

    if right.requires_grad:

        def grad_fn_right(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            return left.data * grad
        depends_on.append(Dependency(tensor=right, grad_fn=grad_fn_right))

    requires_grad = left.requires_grad or right.requires_grad

    return Tensor(data=data, depends_on=depends_on, requires_grad=requires_grad)


def power(inp: Tensor, exponent: int) -> Tensor:
    # TODO
    data = inp.data ** exponent

    requires_grad = inp.requires_grad

    depends_on = []
    if requires_grad:

        def grad_fn(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            adj = exponent * (inp.data ** (exponent - 1))
            return adj * grad

        depends_on.append(Dependency(tensor=inp, grad_fn=grad_fn))

    return Tensor(data=data, depends_on=depends_on, requires_grad=requires_grad)


def sigmoid(inp: Tensor) -> Tensor:
    # TODO
    data = special.expit(inp.data)

    requires_grad = inp.requires_grad

    depends_on = []
    if requires_grad:

        def grad_fn(grad: Array) -> Array:
            adj = data * (1 - data)
            #raise NotImplementedError
            return np.multiply(grad, adj)

        depends_on.append(Dependency(tensor=inp, grad_fn=grad_fn))

    return Tensor(data=data, depends_on=depends_on, requires_grad=requires_grad)


def matmul(left: Tensor, right: Tensor) -> Tensor:
    # TODO
    data = np.matmul(left.data, right.data)

    depends_on = []
    if right.requires_grad:

        def grad_fn_right(grad: Array) -> Array:
            # TODO
            #raise NotImplementedError
            n = (len(grad.shape) + len(left.data.T.shape) - len(right.shape)) // 2
            return np.tensordot(left.data.T, grad, axes=(tuple(range(-1, -n - 1, -1)), tuple(range(n))))

        depends_on.append(Dependency(tensor=right, grad_fn=grad_fn_right))

    requires_grad = left.requires_grad or right.requires_grad

    return Tensor(data=data, depends_on=depends_on, requires_grad=requires_grad)


class SGD:
    def __init__(self, parameters: list, lr: float = 1e-3) -> None:
        self.parameters = parameters
        self.lr = lr

    def step(self):
        # TODO
        #raise NotImplementedError
        self.parameters.data -= self.parameters.grad.data*self.lr

    def zero_grad(self):
        # TODO
        #raise NotImplementedError
        self.parameters.zero_grad_()


def mse_loss(inp: Tensor, target: Tensor) -> Tensor:
    # TODO
    #raise NotImplementedError
    return ((inp - target) ** 2).sum()

