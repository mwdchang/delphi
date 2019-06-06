import torch
import importlib
import numpy as np

from delphi.GrFN.networks import GroundedFunctionNetwork


def test_petpt_torch(N, use_gpu=False):
    lambdas = importlib.__import__("PETPT_torch_lambdas")
    json_filename = "../../tests/data/program_analysis/PETPT.json"
    G = GroundedFunctionNetwork.from_json_and_lambdas(json_filename, lambdas)

    inputs = {
        "petpt::msalb_-1": torch.randint(0, 1, (N,), dtype=torch.double),
        "petpt::srad_-1": torch.randint(1, 20, (N,), dtype=torch.double),
        "petpt::tmax_-1": torch.randint(-30, 60, (N,), dtype=torch.double),
        "petpt::tmin_-1": torch.randint(-30, 60, (N,), dtype=torch.double),
        "petpt::xhlai_-1": torch.randint(0, 20, (N,), dtype=torch.double),
    }

    if use_gpu:
        inputs = {k: v.cuda() for k, v in inputs.items()}

    print(f"Running PETPT with Torch for {N} samples {'w/ GPU' if use_gpu else 'w/ CPU'}")
    G.run(inputs, torch_size=N)


def test_petasce_torch_execution(N, use_gpu=False):
    lambdas = importlib.__import__("PETASCE_torch_lambdas")
    json_filename = "../../tests/data/program_analysis/PETASCE_simple.json"
    G = GroundedFunctionNetwork.from_json_and_lambdas(json_filename, lambdas)

    inputs = {
        "petasce::doy_-1": torch.randint(1, 100, (N,), dtype=torch.double),
        "petasce::meevp_-1": np.where(np.random.rand(N) >= 0.5, 'A', 'W'),
        "petasce::msalb_-1": torch.randint(0, 1, (N,), dtype=torch.double),
        "petasce::srad_-1": torch.randint(1, 30, (N,), dtype=torch.double),
        "petasce::tmax_-1": torch.randint(-30, 60, (N,), dtype=torch.double),
        "petasce::tmin_-1": torch.randint(-30, 60, (N,), dtype=torch.double),
        "petasce::xhlai_-1": torch.randint(0, 20, (N,), dtype=torch.double),
        "petasce::tdew_-1": torch.randint(-30, 60, (N,), dtype=torch.double),
        "petasce::windht_-1": torch.randint(0, 10, (N,), dtype=torch.double),
        "petasce::windrun_-1": torch.randint(0, 900, (N,), dtype=torch.double),
        "petasce::xlat_-1": torch.randint(0, 90, (N,), dtype=torch.double),
        "petasce::xelev_-1": torch.randint(0, 6000, (N,), dtype=torch.double),
        "petasce::canht_-1": torch.randint(1, 3, (N,), dtype=torch.double),
    }

    if use_gpu:
        inputs = {k: v if isinstance(v, np.ndarray) else v.cuda() for k, v in inputs.items()}

    print(f"Running PETASCE with Torch for {N} samples {'w/ GPU' if use_gpu else 'w/ CPU'}")
    G.run(inputs, torch_size=N)


test_petpt_torch(int(1e7))
test_petasce_torch_execution(int(1e6))
test_petpt_torch(int(1e7), use_gpu=True)
test_petasce_torch_execution(int(1e6), use_gpu=True)
