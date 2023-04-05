------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       UNDO / REDO OPERATIONS                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
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
-- DESCRIPTION:
-- 

with ada.text_io;						use ada.text_io;
with ada.containers;					use ada.containers;

with et_general;						use et_general;
with et_schematic;						use et_schematic;
with et_nets;							use et_nets;
with et_modes.schematic;
with et_modes.board;
with et_time;							use et_time;


package body et_undo_redo is


	use pac_net_commit;
	
	
	procedure commit ( -- in schematic domain
		stage	: in type_commit_stage;
		verb	: in et_modes.schematic.type_verb;
		noun	: in et_modes.schematic.type_noun;
		lth		: in type_log_level)
	is
		use et_schematic;
		use et_modes.schematic;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 
		begin
			increment (module.commit_index);			

			case verb is
				when VERB_DRAW | VERB_DELETE | VERB_DRAG => -- CS others

					case noun is
						when NOUN_NET =>
							log (text => "nets", level => lth + 1);
							
							module.net_commits.dos.append (
								make_commit (module.commit_index, stage, module.nets));

						when others =>
							null;
					end case;
					
				when others =>
					null;
			end case;
		end query_module;
		

	begin
		log (text => "commit in schematic (" 
			 & to_string (stage) & " / " & to_string (verb) & " " & to_string (noun) & ")",
			 level => lth + 1);
		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

		log_indentation_down;
	end commit;



	

	procedure commit ( -- in board domain
		stage	: in type_commit_stage;
		verb	: in et_modes.board.type_verb;
		noun	: in et_modes.board.type_noun;
		lth		: in type_log_level)
	is
		use et_schematic;
		use et_modes.board;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 
		begin
			increment (module.commit_index);	

			case verb is
				when VERB_DELETE | VERB_ROUTE | VERB_RIPUP | VERB_MOVE => -- CS others

					case noun is
						when NOUN_NET | NOUN_TRACK =>
							log (text => "nets", level => lth + 1);
							
							module.net_commits.dos.append (
								make_commit (module.commit_index, stage, module.nets));
								-- CS In order to save memory, do not commit the fill lines of zones ?

						when others =>
							null;
					end case;
					
				when others =>
					null;
			end case;
		end query_module;
		

	begin
		log (text => "commit in board (" 
			 & to_string (stage) & " / " & to_string (verb) & " " & to_string (noun) & ")",
			 level => lth + 1);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

		log_indentation_down;
	end commit;



	procedure undo (
		message	: in out pac_undo_message.bounded_string;
		lth		: in type_log_level)
	is
		use pac_undo_message;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			-- Backup places for pre- and post-commits:
			pre_commit, post_commit : type_commit;

			-- After a successful redo-operation, this flag is set.
			done : boolean := false;

			nothing_to_do : constant string := "nothing to undo";

			
			procedure undo_nets is 
				use pac_net_commits;
				dos		: pac_net_commits.list renames module.net_commits.dos;
				redos	: pac_net_commits.list renames module.net_commits.redos;
			begin
				if dos.last_element.index = module.commit_index then

					log (text => "nets", level => lth + 1);
					
					-- Backup post-commit and delete the original:
					post_commit := dos.last_element;
					dos.delete_last;

					-- Backup pre-commit and delete the original:
					pre_commit := dos.last_element;
					dos.delete_last;

					-- Restore the affected part of the design according 
					-- to the pre-commit:
					module.nets := pre_commit.item;


					-- Put pre- and post commit on redo-stack:
					redos.append (pre_commit);
					redos.append (post_commit);					

					-- Mark the undo-operation as successful:
					done := true;

					-- CS add duration between post-commit and current time (like 2 minutes ago)
					--put_line (to_string_full (post_commit.timestamp));
					--message := to_bounded_string ("undo" & to_string (post_commit.timestamp));
				end if;
			end undo_nets;

			
		begin -- query_module
			
			-- An undo-operation is allowed if there have been
			-- commits in the past. Otherwise there would be nothing to undo:
			if module.commit_index > 0 then

				-- Since we have multiple do-stacks (for various kinds of objects),
				-- the do-stack that contains the latest commit must be processed.
				-- All other do-stacks remain untouched:

				-- Search in nets:
				undo_nets;				

				if not done then
					null;
					-- CS Search other stacks:
				end if;

				-- CS if done then ?
				decrement (module.commit_index, 2);

				
				-- end if;
			else
				log (text => nothing_to_do, level => lth + 1);
				message := to_bounded_string (nothing_to_do);
			end if;			
		end query_module;

		
	begin
		log (text => "undo", level => lth);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

		log_indentation_down;
	end undo;
	



	procedure redo (
		message	: in out pac_redo_message.bounded_string;
		lth		: in type_log_level)
	is
		use pac_redo_message;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 		
			-- Contains the index of the latest commit:
			commit_index : constant type_commit_index := module.commit_index + 2;

			-- Backup places for pre- and post-commits:
			pre_commit, post_commit : type_commit;

			-- After a successful redo-operation, this flag is set.
			done : boolean := false;

			nothing_to_do : constant string := "nothing to redo";
			
			
			procedure redo_nets is
				use pac_net_commits;				
				dos		: pac_net_commits.list renames module.net_commits.dos;
				redos	: pac_net_commits.list renames module.net_commits.redos;
			begin
				-- If there are no commits on the redo-stack, then there is
				-- nothing to do.
				if not redos.is_empty then

					-- Do the redo-operation if the last commit is 
					-- the latest among all commits:
					if redos.last_element.index = commit_index then

						log (text => "nets", level => lth + 1);
						
						-- Backup post-commit and delete the original:
						post_commit := redos.last_element;
						redos.delete_last;

						-- Backup pre-commit and delete the original:
						pre_commit := redos.last_element;
						redos.delete_last;

						
						-- Put pre- and post-commit back to dos-stack:
						dos.append (pre_commit);
						dos.append (post_commit);

						-- Restore design according to the post-commit:
						module.nets := post_commit.item;

						-- Mark the redo-operation as successful:
						done := true;
					end if;
				end if;
			end redo_nets;
			
				
		begin -- query_module

			-- Since we have multiple redo-stacks (for various kinds of objects),
			-- the redo-stack that contains the latest commit must be processed.
			-- All other redo-stacks remain untouched:

			-- Search in nets:
			redo_nets;

			if not done then
				null;
				-- CS other redo-stacks like
				-- CS elsif not module.silkscreen_commits.redos.is_empty then
			end if;
				
			if done then
				increment (module.commit_index, 2);
			else
				log (text => nothing_to_do, level => lth + 1);
				message := to_bounded_string (nothing_to_do);
			end if;
		end query_module;

		
	begin
		log (text => "redo", level => lth);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> current_active_module,
			process		=> query_module'access);

		log_indentation_down;		
	end redo;

	
end et_undo_redo;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
