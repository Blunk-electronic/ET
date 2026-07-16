.. _package_models:

.. .. include:: placeholders.rst


Device Packages (or Footprints)
===============================

Importing and Syntax Check
--------------------------

In Headless Mode
^^^^^^^^^^^^^^^^

	
	.. parsed-literal::
		
		$ et --open-package libraries/packages/S_0805.pac


	.. parsed-literal::
		
		$ et --open-package libraries/packages/S_0805.pac --log-level 4

	
	.. parsed-literal::
		
		$ et --open-package libraries/packages/S_0805.pac --save-package-as tmp/test.pac





Create a Package (or Footprint)
-------------------------------

Packages can be real or virtual. Virtual components are things like testpoints or edge connectors.
By default a real package will be created.
The newly created package should be saved right away.
To create a native package drawing like 'S_0805.pac' run this command: 

	.. parsed-literal::
		
		$ et --create-package --package-appearance real --save-package-as tmp/dummy_S_0805.pac


	.. parsed-literal::
		
		$ et --create-package --package-appearance virtual --save-package-as tmp/dummy_connector.pac


Since the appearance has a default, it can be omitted:

	.. parsed-literal::
		
		$ et --create-package --save-package-as tmp/dummy_S_0805.pac







Opening an ET native package (or footprint)
-------------------------------------------

To open a native package drawing like 'S_0805.pac' run this command: 

	.. parsed-literal::
		
		$ et --open-package libraries/packages/S_0805.pac


The package can also be saved under a different name at a different place:

	.. parsed-literal::

		$ et --open-package libraries/packages/S_0805.pac --save-package-as tmp/dummy_S_0805.pac


Opening a package includes syntax checking. See the report for details.

