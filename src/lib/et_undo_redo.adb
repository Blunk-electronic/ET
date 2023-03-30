------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       UNDO / REDO OPERATIONS                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2023 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- DESCRIPTION:
-- 

with ada.text_io;						use ada.text_io;

with et_general;						use et_general;
with et_schematic;						use et_schematic;


package body et_undo_redo is


	procedure commit (
		verb	: in et_modes.schematic.type_verb;
		noun	: in et_modes.schematic.type_noun)
	is
		use et_schematic;
		-- use pac_commit_net;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			
		begin
			-- module.net_commits.append (make_commit (module.nets));
			null;
		end query_module;

	begin
		put_line ("commit in schematic");
		
		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

	end commit;



	

	procedure commit (
		verb	: in et_modes.board.type_verb;
		noun	: in et_modes.board.type_noun)
	is
		use et_schematic;
		-- use pac_commit_net;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			
		begin
			-- module.net_commits.append (make_commit (module.nets));
			null;
		end query_module;

	begin
		put_line ("commit in board");
		
		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

	end commit;



	procedure undo is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			
		begin
			-- module.net_commits.append (make_commit (module.nets));
			-- module.nets.clear;
			null;
		end query_module;

		
	begin
		put_line ("undo");

		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);
		
	end undo;
	



	procedure redo is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			
		begin
			-- module.net_commits.append (make_commit (module.nets));
			-- module.nets.clear;
			null;
		end query_module;

		
	begin
		put_line ("redo");

		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);
		
	end redo;

	
end et_undo_redo;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
