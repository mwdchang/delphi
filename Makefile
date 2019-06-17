.PHONY: docs

docs:
	cd docs; make html

test:
	pytest \
	  --cov-report term-missing:skip-covered --cov=delphi\
	  --doctest-module\
	  --ignore=delphi/analysis/sensitivity/tests\
	  --ignore=delphi/translators/for2py/data\
	  --ignore=tests/data\
	  delphi tests
	rm dbn_sampled_sequences.csv bmi_config.txt delphi_model.pkl

pypi_upload:
	rm -rf dist
	python setup.py sdist bdist_wheel
	twine upload dist/*

clean:
	rm -rf build dist
	rm *.json *.pkl *.csv

push_test_data:
	scp delphi.db vision:public_html
