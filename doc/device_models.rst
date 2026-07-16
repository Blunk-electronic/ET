.. _device_models:

.. .. include:: placeholders.rst


Device Models
=============

A device is the compound of symbol(s) and package(s).


Importing and Syntax Check
--------------------------

In Headless Mode
^^^^^^^^^^^^^^^^

	
	.. parsed-literal::
		
		$ et --open-device libraries/devices/gnd.dev


	.. parsed-literal::
		
		$ et --open-device libraries/devices/capacitor_pol.dev --log-level 4

	
	.. parsed-literal::
		
		$ et --open-device libraries/devices/capacitor_pol.dev --save-device-as test.dev





Create a Device
---------------

	.. parsed-literal::

		$ et --create-device --device-appearance pcb --save-device-as tmp/TL084D.dev


	.. parsed-literal::

		$ et --create-device --device-appearance virtual --save-device-as tmp/gnd.dev


Since the appearance has a default, it can be omitted:

	.. parsed-literal::

		$ et --create-device --device-appearance pcb --save-device-as tmp/TL084D.dev
