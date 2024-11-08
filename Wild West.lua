
local camera = game.Workspace.CurrentCamera
local Animals = game.Workspace.WORKSPACE_Entities.Animals
local Geometry = game.Workspace.WORKSPACE_Geometry
local Table = {
    ["Models"] = {}
}

local function cachedthread()
    local cachedchildren = {LastUpdate = 0}
    local animalModels = {}
    local regionList = {}
    local treesList = {}
    local trunkList = {}

    while true do
        local current = tick()

        if current - cachedchildren.LastUpdate > 1 then
            cachedchildren.LastUpdate = current
            
            -- Clear all tables
            table.clear(animalModels)
            table.clear(regionList)
            table.clear(treesList)
            table.clear(trunkList)
            
            for _, v in ipairs(Animals:GetChildren()) do
                if v.ClassName == "Model" and string.match(v.Name, "Thunder") then
                    table.insert(animalModels, v)
                end
            end
            
            for _, v in ipairs(Geometry:GetChildren()) do
                if string.match(v.Name, "REGION") then
                    local Trees = v:FindFirstChild("Trees") or v:FindFirstChild("Vegetation")
                    if Trees then
                        table.insert(regionList, Trees)
                    end
                end
            end
            
            for _, Trees in ipairs(regionList) do
                local TreesChildren = Trees:GetChildren()
                for _, TreesChild in ipairs(TreesChildren) do
                    table.insert(treesList, TreesChild)
                end
            end
            

            for _, v in ipairs(treesList) do
                local Trunk = v:FindFirstChild("Trunk") or v:FindFirstChild("Cactus")
                if Trunk then
                    if Trunk:FindFirstChildOfClass("ParticleEmitter") then
                        table.insert(trunkList, v)
                    end
                end
            end

            for _, v in ipairs(animalModels) do
                table.insert(trunkList, v)
            end
            
            for _, v in ipairs(trunkList) do
                if string.match(v.Name, "Thunder") or string.match(v.Name, "Tree") or string.match(v.Name, "Cactus") or string.match(v.Name, "wood") then
                    local primaryPart = v:FindFirstChildOfClass("Part") or v:FindFirstChildOfClass("UnionOperation") or v:FindFirstChildOfClass("MeshPart")
                    if primaryPart and not Table.Models[v] then
                        Table.Models[v] = {
                            ["PrimaryPart"] = primaryPart,
                            ["Drawing"] = Drawing.new("Text")
                        }
                    end
                end
            end
        end
        wait(1)
    end
end

local function renderthread()
    while true do
        for i, v in pairs(Table.Models) do
            local primaryPart = v["PrimaryPart"]
            local drawing = v["Drawing"]

            if primaryPart and primaryPart.CFrame and primaryPart.Parent and drawing then
                local Trunk = primaryPart.Parent:FindFirstChild("Trunk") or primaryPart.Parent:FindFirstChild("Cactus")
                local hasParticle = Trunk and Trunk:FindFirstChildOfClass("ParticleEmitter")
                
                if primaryPart.Parent and not hasParticle and not string.match(primaryPart.Parent.Name, "Thunder") then
                    drawing:Remove()
                    Table.Models[i] = nil
                else
                    local position = primaryPart.CFrame.Position
                    local pos2d, onscreen = camera:WorldToScreenPoint(position)
                    local distance = math.floor(math.sqrt((position.x - camera.CFrame.Position.x)^2 + (position.y - camera.CFrame.Position.y)^2 + (position.z - camera.CFrame.Position.z)^2))
                    if distance and primaryPart.Parent then
                        drawing.Text = string.format("[%s][%d]", primaryPart.Parent.Name, distance)
                        drawing.Position = {pos2d.x, pos2d.y}
                        drawing.Color = {255, 255, 255}
                        drawing.Size = 15
                        drawing.Font = 2
                        drawing.Outline = true
                        drawing.Visible = onscreen
                        drawing.OutlineColor = {0, 0, 0}
                        drawing.Center = true
                        drawing.Font = 2
                    end
                end
            else  
                drawing:Remove()
                Table.Models[i] = nil
            end
        end
        wait()
    end
end

spawn(function()
    local success, error = pcall(cachedthread)
    if not success then
        warn(error)
    end
end)
spawn(function()
    local success, error = pcall(renderthread)
    if not success then
        warn(error)
    end
end)