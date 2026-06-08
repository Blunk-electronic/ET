.. _schematic_assembly_variants:

.. include:: placeholders.rst


Assembly Variants
-----------------


Create Variant
^^^^^^^^^^^^^^

	.. code-block::

		create variant low_cost



Describe Variant
^^^^^^^^^^^^^^^^

	.. code-block::

		describe variant low_cost "This is the low budget version."


Delete Variant
^^^^^^^^^^^^^^

	.. code-block::

		delete variant low_cost



Mount Device
^^^^^^^^^^^^

	.. code-block::

		mount device low_cost R1 270R R_PAC_S_0805_VAL_270R


	.. code-block::

		mount device low_cost R1 270R R_PAC_S_0805_VAL_270R brightnesss_control



Unmount Device
^^^^^^^^^^^^^^

If a device is not to be mounted in a certain variant:

	.. code-block::

		unmount device low_cost R2



Remove Device
^^^^^^^^^^^^^

If a device is to be removed from the assembly variant:

	.. code-block::

		remove device low_cost R2


