.. _schematic:

****************
Schematic Editor
****************


.. |VNS| replace:: Verb-Noun key sequence:

+++++++++++++++++
Devices and Units
+++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------


#. Add device

	.. code-block::

		add device

	|VNS| a d

	
#. Rename device

	.. code-block::

		rename device R1 R9
	
	
#. Delete unit

	.. code-block::

		delete unit IC1 [B]

	|VNS| del u
	

#. Drag unit

	.. code-block::

		drag unit IC1 [A]

	|VNS| d u


#. Invoke unit

	.. code-block::

		invoke unit IC1 [B]

	|VNS| i u

	
#. Move unit

	.. code-block::

		move unit IC1 [A]

	|VNS| m u
	

#. Rotate unit

	.. code-block::

		rotate unit IC1 [A]

	|VNS| r u


#. Set package variant

	.. code-block::

		set variant IC1 [S_0805]

	|VNS| s a
	
	
#. Set value

	.. code-block::

		set value R1 [100R]
		
	|VNS| s v

	
#. Set partcode

	.. code-block::

		set partcode R1 [R_PAC_S_0805_VAL_100R}

	|VNS| s p

	
#. Set purpose

	.. code-block::

		set purpose R1 [brightness]

	|VNS| s p

	
Placeholders for Name, Value, Partcode and Purpose
++++++++++++++++++++++++++++++++++++++++++++++++++

	
#. Move name

	.. code-block::

		move name IC1 [A]

	|VNS| m n
		

#. Move value

	.. code-block::

		move value IC1 [A]

	|VNS| m v
		

#. Move purpose

	.. code-block::

		move purpose RN1 [A]

	|VNS| m p
	
	
#. Rotate name

	.. code-block::

		rotate name IC1 [A]

	|VNS| r n
		

#. Rotate value

	.. code-block::

		rotate value IC1 [A]

	|VNS| r v
		

#. Rotate purpose

	.. code-block::

		rotate purpose RN1 [A]

	|VNS| r p
	

		
+++++++++++++++++++
Nets and Net Labels
+++++++++++++++++++

Commands and Verb-Noun key sequences
------------------------------------

#. Draw net

	|VNS| d n

	change bend style: left mouse click or key b

	
#. Rename net

	On all sheets:
	
	.. code-block::

		rename net RESET_N RST_N

.. 	On the current active sheet

.. 	Where left click or space happens


#. Drag net segment

	|VNS| g n

	
#. Delete net segment

	|VNS| del n

	
#. Delete whole net

	.. code-block::

		delete net RESET_N
	
	
#. Place simple label

	|VNS| p l

	Rotate: right mouse click or key r
	
	
#. Place tag label

	|VNS| p L
	
	
#. Move label

	|VNS| m l

	
#. Delete label

	|VNS| del l

	
