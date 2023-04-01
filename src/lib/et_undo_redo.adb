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

with et_general;						use et_general;
with et_schematic;						use et_schematic;
with et_nets;							use et_nets;
with et_modes.schematic;
with et_modes.board;
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
		is 
			use et_modes.schematic;
		begin
			increment (module.commit_index);			

			case verb is
				when VERB_DRAW | VERB_DELETE | VERB_DRAG => -- CS others

					case noun is
						when NOUN_NET =>
							module.net_commits.dos.append (
								make_commit (module.commit_index, stage, module.nets));

						when others =>
							null;
					end case;
					
				when others =>
					null;
			end case;

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
		is 
			use et_modes.board;
		begin
			increment (module.commit_index);	

			case verb is
				when VERB_DELETE | VERB_ROUTE | VERB_RIPUP | VERB_MOVE => -- CS others

					case noun is
						when NOUN_TRACK =>
							module.net_commits.dos.append (
								make_commit (module.commit_index, stage, module.nets));

						when others =>
							null;
					end case;
					
				when others =>
					null;
			end case;
			
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

				-- Locate the undo-stack that contains the latest commit:

				-- Search in nets:
				if module.net_commits.dos.last_element.index = module.commit_index then

					-- undo post-commit:
					module.net_commits.redos.append (module.net_commits.dos.last_element);
					module.net_commits.dos.delete_last;

					-- undo pre-commit:
					-- restore state
					module.nets := module.net_commits.dos.last_element.item;
					module.net_commits.dos.delete_last;
				end if;

				-- put_line ("stack height A:" & count_type'image (module.net_commits.length));

				-- CS Search other stacks:




				
				decrement (module.commit_index, 2);
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

			commit_index : type_commit_index := module.commit_index + 2;
		begin
			-- increment (module.commit_index);
			
			-- Locate the redo-stack that contains the latest commit:
			
			if not module.net_commits.redos.is_empty then

				if module.net_commits.redos.last_element.index = commit_index then

					-- Move latest commit back to dos-stack:
					module.net_commits.dos.append (module.net_commits.redos.last_element);

					-- Restore design:
					module.nets := module.net_commits.redos.last_element.item;
					
					module.net_commits.redos.delete_last;
				end if;
								
				
			else
				-- CS other redo-stacks like
				-- CS elsif not module.silscreen_commits.redos.is_empty then
				
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
