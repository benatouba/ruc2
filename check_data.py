import uc2data

ds = uc2data.Dataset("./iop4_test.nc")

ds.uc2_check()  # do the check

print("UC2 data standard test result:\n")
#  print(ds.check_result)  # the results are saved within the dataset's attribute check_result
print(ds.check_result.errors)  # get just the errors
print(ds.check_result.warnings)  # get just the warnings
