------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NET CLASSES                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:
--		- separate in two packages things related to board and device package.


with ada.containers; 				use ada.containers;
with ada.containers.ordered_maps;

with et_net_class;					use et_net_class;
with et_net_class_name;				use et_net_class_name;
-- with et_net_class_description;		use et_net_class_description;


package et_net_classes is


	use pac_net_class_name;
	
	package pac_net_classes is new ordered_maps (
		key_type		=> pac_net_class_name.bounded_string,
		element_type	=> type_net_class);
	

	use pac_net_classes;


	
	function get_net_class (
		class_cursor	: in pac_net_classes.cursor)
		return type_net_class;

		
	function get_net_class_name (
		class_cursor	: in pac_net_classes.cursor)
		return pac_net_class_name.bounded_string;


	function get_net_class_name (
		class_cursor	: in pac_net_classes.cursor)
		return string;

	
end et_net_classes;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
