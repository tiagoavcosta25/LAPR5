using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.Connections;
using DDDNetCore.Domain.Connections.DTOS;
using DDDNetCore.Controllers;
using Moq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using DDDSample1.Domain.Shared;

namespace DDDNetCore.Tests.Controllers
{
    public class ConnectionControllerTest
    {
        [Fact]
        public async Task GetReachablePlayers_ReturnsPlayersList()
        {
            // Arrange
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            List<PlayerDto> lst = new List<PlayerDto>{obj};
            string playerEmail = "test2@email.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(serv => serv.GetReachablePlayers(playerEmail))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetReachablePlayers(playerEmail);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, returnValue);
        }

        [Fact]
        public async Task GetMutualFriends_ReturnsPlayersList()
        {
            // Arrange
            GetMutualFriendsDto dto = new GetMutualFriendsDto("test@email.com");
            PlayerDto obj = new PlayerDto(new System.Guid(), "john", "test@email.com", "987654321", 2001, 1, 17, "joyful", "www.facebook.com/john-doe", 
            "www.linkedin.com/john-doe", new List<string>{"tag1"});
            List<PlayerDto> lst = new List<PlayerDto>{obj};
            string playerEmail = "test2@email.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(serv => serv.GetMutualFriends(playerEmail, dto))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetMutualFriends(playerEmail, dto);

            // Assert
            var returnValue = Assert.IsType<List<PlayerDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, returnValue);
        }

        [Fact]
        public async Task GetNetwork_ReturnsPlayersList()
        {
            // Arrange
            GetNetworkDto dto = new GetNetworkDto(2);
            ConnectionDto obj = new ConnectionDto("1", "p1", "p2", 1, new List<string>{"tag1"});
            List<ConnectionDto> lst = new List<ConnectionDto>{obj};
            string playerEmail = "test2@email.com";

            var mockServ = new Mock<IConnectionService>();
            mockServ.Setup(serv => serv.GetNetwork(playerEmail, dto))
                .ReturnsAsync(lst).Verifiable();
            var controller = new ConnectionsController(mockServ.Object);

            // Act
            var result = await controller.GetNetwork(playerEmail, dto);

            // Assert
            var returnValue = Assert.IsType<List<ConnectionDto>>(result.Value);
            mockServ.Verify();
            
            Assert.Equal(lst, returnValue);
        }
    }
}