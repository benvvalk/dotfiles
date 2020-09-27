#r "C:\Program Files\workspacer\workspacer.Shared.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.Bar\workspacer.Bar.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.ActionMenu\workspacer.ActionMenu.dll"
#r "C:\Program Files\workspacer\plugins\workspacer.FocusIndicator\workspacer.FocusIndicator.dll"

using System;
using workspacer;
using workspacer.Bar;
using workspacer.ActionMenu;
using workspacer.FocusIndicator;

/// <summary>
/// Build an interactive menu listing the titles of all
/// windows that are currently managed by Workspacer.
/// </summary>
ActionMenuItemBuilder BuildWindowMenu(ActionMenuPlugin actionMenu, IConfigContext context)
{
    var builder = actionMenu.Create();
    var workspaces = context.WorkspaceContainer.GetAllWorkspaces();
    foreach (var workspace in workspaces)
    {
        foreach (var window in workspace.ManagedWindows)
        {
            var text = $"[{workspace.Name}] {window.Title}";
            builder.Add(text, () => context.Workspaces.SwitchToWindow(window));
        }
    }
    return builder;
}

Action<IConfigContext> doConfig = (context) =>
{
    context.AddBar();
    context.AddFocusIndicator();
    var actionMenu = context.AddActionMenu();

    context.WorkspaceContainer.CreateWorkspaces("one", "two", "three", "four", "five");
};
return doConfig;