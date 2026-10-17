------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              CONTOURS                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

-- with ada.containers;			use ada.containers;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;

-- with et_primitive_objects;		use et_primitive_objects;
with et_basic_objects;
with et_basic_objects.circle;
with et_basic_objects.arc;


generic

	with package pac_objects is new et_basic_objects (<>);
	with package pac_circle is new pac_objects.circle;
	with package pac_arc is new pac_objects.arc;

package et_contours is
	use pac_objects;
	use pac_geometry;

	a : natural := 0;

	l : type_line;

end et_contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
