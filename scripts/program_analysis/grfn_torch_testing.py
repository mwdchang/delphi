import torch
import importlib
import numpy as np

from delphi.GrFN.networks import GroundedFunctionNetwork


def test_petpt_torch():
    lambdas = importlib.__import__("PETPT_torch_lambdas")
    json_filename = "../../tests/data/program_analysis/PETPT.json"
    G = GroundedFunctionNetwork.from_json_and_lambdas(json_filename, lambdas)

    num_samples = int(1e7)
    inputs = {
        "petpt::msalb_-1": torch.linspace(0, 1, num_samples),
        "petpt::srad_-1": torch.linspace(1, 20, num_samples),
        "petpt::tmax_-1": torch.linspace(-30, 60, num_samples),
        "petpt::tmin_-1": torch.linspace(-30, 60, num_samples),
        "petpt::xhlai_-1": torch.linspace(0, 20, num_samples),
    }
    print(f"Running PETPT with Torch for {num_samples} samples")
    G.run(inputs, torch_size=num_samples)


def test_petasce_torch_execution():
    lambdas = importlib.__import__("PETASCE_torch_lambdas")
    json_filename = "../../tests/data/program_analysis/PETASCE_simple.json"
    G = GroundedFunctionNetwork.from_json_and_lambdas(json_filename, lambdas)

    N = int(1e7)
    samples = {
        "petasce::doy_-1": np.random.randint(1, 100, N),
        "petasce::meevp_-1": np.where(np.random.rand(N) >= 0.5, 'A', 'W'),
        "petasce::msalb_-1": np.random.uniform(0, 1, N),
        "petasce::srad_-1": np.random.uniform(1, 30, N),
        "petasce::tmax_-1": np.random.uniform(-30, 60, N),
        "petasce::tmin_-1": np.random.uniform(-30, 60, N),
        "petasce::xhlai_-1": np.random.uniform(0, 20, N),
        "petasce::tdew_-1": np.random.uniform(-30, 60, N),
        "petasce::windht_-1": np.random.uniform(0, 10, N),
        "petasce::windrun_-1": np.random.uniform(0, 900, N),
        "petasce::xlat_-1": np.random.uniform(0, 90, N),
        "petasce::xelev_-1": np.random.uniform(0, 6000, N),
        "petasce::canht_-1": np.random.uniform(0.001, 3, N),
    }

    values = {
        k: torch.tensor(v, dtype=torch.double) if v.dtype != "<U1" else v
        for k, v in samples.items()
    }

    print(f"Running PETASCE with Torch for {N} samples")
    G.run(values, torch_size=N)


test_petpt_torch()
test_petasce_torch_execution()
