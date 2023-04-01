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
with ada.containers;					use ada.containers;
with ada.containers.vectors;

with et_general;						use et_general;
with et_schematic;						use et_schematic;
with et_nets;							use et_nets;

with et_commit;							use et_commit;


package body et_undo_redo is


	procedure commit (
		stage	: in type_commit_stage;
		verb	: in et_modes.schematic.type_verb;
		noun	: in et_modes.schematic.type_noun)
	is
		use et_schematic;
		use pac_net_commit;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			increment (module.commit_index);			
			module.net_commits.dos.append (make_commit (module.nets));
			-- put_line ("stack height:" & count_type'image (module.net_commits.length));
		end query_module;

	begin
		put_line ("commit in schematic");
		
		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

	end commit;



	

	procedure commit (
		stage	: in type_commit_stage;
		verb	: in et_modes.board.type_verb;
		noun	: in et_modes.board.type_noun)
	is
		use et_schematic;
		use pac_net_commit;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			increment (module.commit_index);			
			module.net_commits.dos.append (make_commit (module.nets));
			-- put_line ("stack height:" & count_type'image (module.net_commits.length));
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
			use pac_net_commit;
			use pac_net_commits;
		begin
			if module.commit_index > 0 then
				decrement (module.commit_index);

				-- put_line ("stack height A:" & count_type'image (module.net_commits.length));
				-- put_line ("stack last idx:" & extended_index'image (module.net_commits.last_index));
				
				module.net_commits.redos.append (make_commit (module.net_commits.dos.last_element.item));
				module.net_commits.dos.delete_last;

				
				decrement (module.commit_index);
				module.nets := module.net_commits.dos.last_element.item;
				module.net_commits.dos.delete_last;
				-- put_line ("stack height B:" & count_type'image (module.net_commits.length));
			else
				put_line ("nothing to undo");
			end if;
			
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
			use pac_net_commit;
			use pac_net_commits;
		begin
			if module.net_commits.redos.length > 0 then
				increment (module.commit_index);			
				
				module.nets := module.net_commits.redos.last_element.item;
				module.net_commits.dos.append (make_commit (module.nets));
				
				module.net_commits.redos.delete_last;
			else
				put_line ("nothing to redo");
			end if;
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
