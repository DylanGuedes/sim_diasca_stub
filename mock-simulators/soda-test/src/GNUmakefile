MOCK_SIMULATORS_TOP = ../..


.PHONY: all demo demo-batch batch test-platform-integration


all:


demo:
	@$(MAKE) soda_stochastic_integration_run


demo-batch:
	@$(MAKE) soda_stochastic_integration_run CMD_LINE_OPT="--batch"


batch: demo-batch


test-platform-integration:
	@$(MAKE) soda_platform_integration_run CMD_LINE_OPT="--batch -start-verbatim-options --simulation-instance-id 944"


include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
